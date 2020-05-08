// { dg-do preprocess { target c++ } }
// { dg-options "-std=c++14" }
// { dg-additional-options -fdirectives-only }

012'bcd
#define A 1
// '
#ifndef A
#error Fell into first char const
#endif
enum { A = 195'936'478 }; 'a'
#define AA 1
			  // 'a
#ifndef AA
#error Fell into early char const
#endif

012\
'bcd
#define B 1
// '
#ifndef B
#error Fell into second char const
#endif

.012'b
#define C 1
// '
#ifndef C
#error Fell into third char const
#endif

.0e+12'b
#define D 1
// '
#ifndef D
#error Fell into fourth char const
#endif
