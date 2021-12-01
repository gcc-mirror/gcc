
version(CRuntime_Microsoft)
{
    extern(C)
    {
        extern __gshared uint _DP_beg;
        extern __gshared uint _DP_end;
        extern __gshared uint _TP_beg;
        extern __gshared uint _TP_end;
    }
    alias _DPbegin = _DP_beg;
    alias _DPend = _DP_end;
    alias _TPbegin = _TP_beg;
    alias _TPend = _TP_end;

    __gshared void[] dataSection;
    shared static this()
    {
        import core.internal.traits : externDFunc;
        alias findImageSection = externDFunc!("rt.sections_win64.findImageSection",
                                              void[] function(string name) nothrow @nogc);
        dataSection = findImageSection(".data");
    }
    
    void[] tlsRange;
    static this()
    {
        import core.internal.traits : externDFunc;
        alias initTLSRanges = externDFunc!("rt.sections_win64.initTLSRanges",
                                              void[] function() nothrow @nogc);
        tlsRange = initTLSRanges();
    }
    
    version = ptrref_supported;
}
else version(Win32)
{
    extern(C)
    {
        extern __gshared void* _DPbegin;
        extern __gshared void* _DPend;
        extern __gshared uint _TPbegin;
        extern __gshared uint _TPend;
        extern int _tlsstart;
        extern int _tlsend;
    }

    void[] tlsRange;
    static this()
    {
		tlsRange = (cast(void*)&_tlsstart)[0.. cast(void*)&_tlsend - cast(void*)&_tlsstart];
	}

    version = ptrref_supported;
}

struct Struct
{
    int x;
    Struct* next;
}

class Class
{
    void* ptr;
}

struct Struc(T)
{
	static T vtls;
	static __gshared T vgshared;
}

__gshared Struct* gsharedStrctPtr2 = new Struct(7, new Struct(8, null));

int tlsInt;
void* tlsVar;

shared int sharedInt;
shared void* sharedVar;
__gshared void* gsharedVar;
__gshared void* gsharedVar2;
immutable int[] arr = [1, 2, 3];
string tlsStr;

__gshared Struct gsharedStrct;
Struct[3] tlsStrcArr;
Class tlsClss;

// expression initializers
string[] strArr = [ "a", "b" ];
__gshared Class gsharedClss = new Class;
__gshared Struct* gsharedStrctPtr = new Struct(7, new Struct(8, null));

debug(PRINT) import core.stdc.stdio;

void main()
{
    version(ptrref_supported)
        testRefPtr();
}

version(ptrref_supported):

bool findTlsPtr(const(void)* ptr)
{
    debug(PRINT) printf("findTlsPtr %p\n", ptr);
    for (uint* p = &_TPbegin; p < &_TPend; p++)
    {
        void* addr = tlsRange.ptr + *p;
        debug(PRINT) printf("  try %p\n", addr);
        assert(*p < tlsRange.length);
        if (addr == ptr)
            return true;
    }
    return false;
}

bool findDataPtr(const(void)* ptr)
{
    debug(PRINT) printf("findDataPtr %p\n", ptr);
    for (auto p = &_DPbegin; p < &_DPend; p++)
    {
        debug(PRINT) printf("  try %p\n", cast(void*) cast(size_t) *p);
        version(CRuntime_Microsoft)
            void* addr = dataSection.ptr + *p;
        else
            void* addr = *p;
        
        if (addr == ptr)
            return true;
    }
    return false;
}

void testRefPtr()
{
    debug(PRINT) printf("&_DPbegin %p\n", &_DPbegin);
    debug(PRINT) printf("&_DPend   %p\n", &_DPend);

    debug(PRINT) printf("&_TPbegin %p\n", &_TPbegin);
    debug(PRINT) printf("&_TPend   %p\n", &_TPend);

    assert(!findDataPtr(cast(void*)&sharedInt));
    assert(!findTlsPtr(&tlsInt));

    assert(findDataPtr(cast(void*)&sharedVar));
    assert(findDataPtr(&gsharedVar));
    assert(findDataPtr(&gsharedStrct.next));
    assert(findDataPtr(&(gsharedClss)));
    assert(findDataPtr(&(gsharedClss.ptr)));

    assert(findTlsPtr(&tlsVar));
    assert(findTlsPtr(&tlsClss));
    assert(findTlsPtr(&tlsStrcArr[0].next));
    assert(findTlsPtr(&tlsStrcArr[1].next));
    assert(findTlsPtr(&tlsStrcArr[2].next));

    assert(!findTlsPtr(cast(size_t*)&tlsStr)); // length
    assert(findTlsPtr(cast(size_t*)&tlsStr + 1)); // ptr
    
    // monitor is manually managed
    assert(!findDataPtr(cast(size_t*)cast(void*)Class.classinfo + 1));
    assert(!findDataPtr(cast(size_t*)cast(void*)Class.classinfo + 1));

    assert(!findDataPtr(&arr));
    assert(!findTlsPtr(&arr));
    assert(!findDataPtr(cast(size_t*)&arr + 1));
    assert(!findTlsPtr(cast(size_t*)&arr + 1));
    
    assert(findDataPtr(cast(size_t*)&strArr[0] + 1)); // ptr in _DATA!
    assert(findDataPtr(cast(size_t*)&strArr[1] + 1)); // ptr in _DATA!
    strArr[1] = "c";

    assert(findDataPtr(&gsharedStrctPtr));
    assert(findDataPtr(&gsharedStrctPtr.next));
    assert(findDataPtr(&gsharedStrctPtr.next.next));

    assert(findDataPtr(&(Struc!(int*).vgshared)));
    assert(!findDataPtr(&(Struc!(int).vgshared)));
    assert(findTlsPtr(&(Struc!(int*).vtls)));
    assert(!findTlsPtr(&(Struc!(int).vtls)));
}
