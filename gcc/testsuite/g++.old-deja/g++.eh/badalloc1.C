// { dg-do run { xfail xstormy16-*-* *-*-darwin* } }
// Copyright (C) 2000, 2002, 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 June 2000 <nathan@codesourcery.com>

// Check we can throw a bad_alloc exception when malloc dies.

typedef __SIZE_TYPE__ size_t;
extern "C" void abort();
extern "C" void *memcpy(void *, const void *, size_t);

// Assume that STACK_SIZE defined implies a system that does not have a
// large data space either, and additionally that we're not linking against
// a shared libstdc++ (which requires quite a bit more initialization space).
#ifdef STACK_SIZE
const int arena_size = 256;
#else
#if defined(__FreeBSD__) || defined(__sun__)
// FreeBSD with threads and Solaris with threads require even more
// space at initialization time.  FreeBSD 5 now requires over 131072 bytes.
const int arena_size = 262144;
#else
const int arena_size = 32768;
#endif
#endif

struct object
{
  size_t size __attribute__((aligned));
};

static char arena[arena_size] __attribute__((aligned));
static size_t pos;

// So we can force a failure when needed.
static int fail;

extern "C" void *malloc (size_t size)
{
  object *p = reinterpret_cast<object *>(&arena[pos]);

  if (fail)
    return 0;

  p->size = size;
  size = (size + __alignof__(object) - 1) & - __alignof__(object);
  pos += size + sizeof(object);

  // Verify that we didn't run out of memory before getting initialized.
  if (pos > arena_size)
    abort ();

  return p + 1;
}

extern "C" void free (void *)
{
}

extern "C" void *realloc (void *p, size_t size)
{
  void *r;

  if (p)
    {
      object *o = reinterpret_cast<object *>(p) - 1;
      size_t old_size = o->size;

      if (old_size >= size)
	{
	  r = p;
	  o->size = size;
	}
      else
	{
	  r = malloc (size);
	  memcpy (r, p, old_size);
	  free (p);
	}
    }
  else
    r = malloc (size);

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
  /* On some systems (including FreeBSD and Solaris 2.10),
     __cxa_get_globals will try to call "malloc" when threads are in
     use.  Therefore, we throw one exception up front so that
     __cxa_get_globals is all set up.  Ideally, this would not be
     necessary, but it is a well-known idiom, and using this technique
     means that we can still validate the fact that exceptions can be
     thrown when malloc fails.  */
  try{fn_throw();}
  catch(int a){}

  fail = 1;

  try{fn_throw();}
  catch(int a){}

  try{fn_rethrow();}
  catch(int a){}

  try{fn_catchthrow();}
  catch(int a){}
  
  return 0;
}
