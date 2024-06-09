/* PR preprocessor/110558 */
/* { dg-do preprocess } */
#define STRINGIZE(x) #x
#define GET_INCLUDE(i) STRINGIZE(has-include-i.h)
/* Spaces surrounding the macro args previously caused a problem for __has_include().  */
#if __has_include(GET_INCLUDE(2)) && __has_include(GET_INCLUDE( 2)) && __has_include(GET_INCLUDE( 2 ))
#include GET_INCLUDE(2)
#include GET_INCLUDE( 2)
#include GET_INCLUDE( 2 )
#else
#error "__has_include did not handle padding properly" /* { dg-bogus "__has_include" } */
#endif
