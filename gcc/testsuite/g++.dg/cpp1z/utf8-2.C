// { dg-options "-std=c++1z" }

#ifndef __cpp_unicode_characters
#error __cpp_unicode_characters not defined
#endif

#if __cpp_unicode_characters != 201411
#error Wrong value for __cpp_unicode_characters
#endif
