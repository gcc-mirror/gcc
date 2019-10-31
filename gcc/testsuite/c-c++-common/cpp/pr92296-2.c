/* PR preprocessor/92296 */
/* { dg-do preprocess } */
/* { dg-options "-Wno-builtin-macro-redefined" } */

#pragma push_macro("__TIMESTAMP__")
#undef __TIMESTAMP__
#define __TIMESTAMP__ "Thu Oct 31 12:00:00 2019"
timestamp1 = __TIMESTAMP__
#pragma pop_macro("__TIMESTAMP__")
timestamp2 = __TIMESTAMP__

#pragma push_macro("__TIME__")
#undef __TIME__
#define __TIME__ "12:00:00"
time1 = __TIME__
#pragma pop_macro("__TIME__")
time2 = __TIME__

#pragma push_macro("__DATE__")
#undef __DATE__
#define __DATE__ "Oct 31 2019"
date1 = __DATE__
#pragma pop_macro("__DATE__")
date2 = __DATE__

#pragma push_macro("__FILE__")
#undef __FILE__
#define __FILE__ "pr92296-3.c"
file1 = __FILE__	/* { dg-final { scan-file pr92296-2.i "file1 = \"pr92296-3.c\"" } } */
#pragma pop_macro("__FILE__")
file2 = __FILE__	/* { dg-final { scan-file-not pr92296-2.i "file2 = \"pr92296-3.c\"" } } */

#pragma push_macro("__BASE_FILE__")
#undef __BASE_FILE__
#define __BASE_FILE__ "pr92296-4.c"
filebase1 = __BASE_FILE__	/* { dg-final { scan-file pr92296-2.i "filebase1 = \"pr92296-4.c\"" } } */
#pragma pop_macro("__BASE_FILE__")
filebase2 = __BASE_FILE__	/* { dg-final { scan-file-not pr92296-2.i "filebase2 = \"pr92296-4.c\"" } } */

#pragma push_macro("__LINE__")
#undef __LINE__		/* { dg-warning "undefining" } */
#define __LINE__ 142
line1 = __LINE__	/* { dg-final { scan-file pr92296-2.i "line1 = 142" } } */
#pragma pop_macro("__LINE__")
line2 = __LINE__	/* { dg-final { scan-file pr92296-2.i "line2 = 45" } } */

#pragma push_macro("__INCLUDE_LEVEL__")
#undef __INCLUDE_LEVEL__	/* { dg-warning "undefining" } */
#define __INCLUDE_LEVEL__ 42
includelevel1 = __INCLUDE_LEVEL__	/* { dg-final { scan-file pr92296-2.i "includelevel1 = 42" } } */
#pragma pop_macro("__INCLUDE_LEVEL__")
includelevel2 = __INCLUDE_LEVEL__	/* { dg-final { scan-file pr92296-2.i "includelevel2 = 0" } } */

#pragma push_macro("__COUNTER__")
#undef __COUNTER__	/* { dg-warning "undefining" } */
#define __COUNTER__ 172
counter1 = __COUNTER__	/* { dg-final { scan-file pr92296-2.i "counter1 = 172" } } */
#pragma pop_macro("__COUNTER__")
counter2 = __COUNTER__	/* { dg-final { scan-file-not pr92296-2.i "counter2 = 172" } } */

#pragma push_macro("__has_attribute")
#undef __has_attribute	/* { dg-warning "undefining" } */
#define __has_attribute(x) 0
hasattr1 = __has_attribute(noreturn)	/* { dg-final { scan-file pr92296-2.i "hasattr1 = 0" } } */
#pragma pop_macro("__has_attribute")
hasattr2 = __has_attribute(noreturn)	/* { dg-final { scan-file-not pr92296-2.i "hasattr2 = 0" } } */

#pragma push_macro("__has_cpp_attribute")
#undef __has_cpp_attribute	/* { dg-warning "undefining" } */
#define __has_cpp_attribute(x) 0
hasattrcpp1 = __has_cpp_attribute(noreturn)	/* { dg-final { scan-file pr92296-2.i "hasattrcpp1 = 0" } } */
#pragma pop_macro("__has_cpp_attribute")
hasattrcpp2 = __has_cpp_attribute(noreturn)	/* { dg-final { scan-file-not pr92296-2.i "hasattrcpp2 = 0" } } */

#pragma push_macro("__has_builtin")
#undef __has_builtin	/* { dg-warning "undefining" } */
#define __has_builtin(x) 0
hasbuiltin1 = __has_builtin(__builtin_expect)	/* { dg-final { scan-file pr92296-2.i "hasbuiltin1 = 0" } } */
#pragma pop_macro("__has_builtin")
hasbuiltin2 = __has_builtin(__builtin_expect)	/* { dg-final { scan-file pr92296-2.i "hasbuiltin2 = 1" } } */
