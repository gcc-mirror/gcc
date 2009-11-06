// { dg-warning "Callback to register pragmas" "" { target *-*-* } 0 }

int some_func (int c);

#pragma GCCPLUGIN sayhello "here" // { dg-warning "'pragma GCCPLUGIN sayhello' outside of function: here" }

int some_func (const char* s)
{
#pragma GCCPLUGIN sayhello "at start" // { dg-warning "'pragma GCCPLUGIN sayhello' from function 'some_func': at start" }

#define DO_PRAGMA(x) _Pragma(#x)
  if (!s)
    {
      DO_PRAGMA(GCCPLUGIN sayhello "in block"); // { dg-warning "'pragma GCCPLUGIN sayhello' from function 'some_func': in block" }
      return 0;
    }
  return 1;
}
