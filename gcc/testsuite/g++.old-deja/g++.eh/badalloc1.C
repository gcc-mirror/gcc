// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 June 2000 <nathan@codesourcery.com>

// Check we can throw a bad_alloc exception when malloc dies

static __SIZE_TYPE__ arena[32767]; // so things can initialize
static int fail;
static unsigned pos;

extern "C" void *malloc (__SIZE_TYPE__ size)
{
  __SIZE_TYPE__ *p = &arena[pos];

  if (fail)
    return 0;
  
  arena[pos] = size;
  size = (size + 4 * sizeof (__SIZE_TYPE__) - 1)
         / sizeof (__SIZE_TYPE__) & ~3; // Yes, this is a hack
  pos += size + 4;
  return p + 4;
}
extern "C" void free (void *)
{
  
}
extern "C" void *realloc (void *p, __SIZE_TYPE__ size)
{
  void *r = malloc (size);
  unsigned int oldSize;
  
  if (r && p)
    {
      oldSize = ((__SIZE_TYPE__ *)p)[-4];
      if (oldSize < size)
        size = oldSize;
      while (size--)
        ((char *)r)[size] = ((char *)p)[size];
    }
  free (p);
  return r;
}

void fn_throw() throw(int)
{
  throw 1;
}

void fn_rethrow() throw(int)
{
  try{fn_throw();}
  catch(int a){
    throw;}
}

void fn_catchthrow() throw(int)
{
  try{fn_throw();}
  catch(int a){
    throw a + 1;}
}

int main()
{
  fail = 1;

  try{fn_throw();}
  catch(int a){}

  try{fn_rethrow();}
  catch(int a){}

  try{fn_catchthrow();}
  catch(int a){}
  
  return 0;
}
