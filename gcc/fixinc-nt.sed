s/!__STDC__/!defined (__STRICT_ANSI__)/g
s/getcwd(char \*, int)/getcwd(char *, size_t)/
s/Format\[\]/Format\[1\]/
s/^#if !defined (__cplusplus)/#if 0/
s/^#define DECLSPEC_IMPORT __declspec(dllimport)/#define DECLSPEC_IMPORT/
