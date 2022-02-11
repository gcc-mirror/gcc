/*
REQUIRED_ARGS: -H -Hd${RESULTS_DIR}/compilable
PERMUTE_ARGS: -d -dw

OUTPUT_FILES: ${RESULTS_DIR}/compilable/test7754.di
TEST_OUTPUT:
---
=== ${RESULTS_DIR}/compilable/test7754.di
// D import file generated from 'compilable/test7754.d'
struct Foo(T)
{
	shared static this()
	{
	}
	static this()
	{
	}
}
---
*/

struct Foo(T)
{
   shared static this()
   {
   }

   static this()
   {
   }
}
