typedef int (*FARPROC)();

typedef int (*LPFN_ACCEPTEX)(void*);
static LPFN_ACCEPTEX acceptex_fn;

int xWSAIoctl(void*);
static void get_fn(FARPROC* fn)
{
    FARPROC func;
    if (!xWSAIoctl( &func))
        *fn = func;
}

void get_fn_pointers()
{
    get_fn((FARPROC*)&acceptex_fn);
}
