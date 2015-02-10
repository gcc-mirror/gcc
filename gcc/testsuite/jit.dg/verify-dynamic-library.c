/* For use by jit-verify-dynamic-library, used by
   test-compile-to-dynamic-library.c.  */
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char **argv)
{
  void *handle;
  void (*hello_world) (const char *name);
  char *error;

  handle = dlopen ("./output-of-test-compile-to-dynamic-library.c.so",
		   RTLD_NOW | RTLD_LOCAL);
  if (!handle)
    {
      fprintf (stderr, "dlopen failed: %s\n", dlerror());
      exit (1);
    }

  /* Clear any existing error */
  dlerror ();

  /* This symbol is from the DSO built by
     test-compile-to-dynamic-library.c.  */
  *(void **) (&hello_world) = dlsym (handle, "hello_world");

  if ((error = dlerror()) != NULL)
    {
      fprintf (stderr, "dlsym failed: %s\n", error);
      exit (2);
    }

  /* Call the function from the generated DSO.  */
  hello_world (argv[0]);

  dlclose (handle);

  return 0;
}
