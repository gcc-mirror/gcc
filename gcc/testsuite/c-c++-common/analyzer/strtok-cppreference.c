/* Example of strtok adapted from:
     https://en.cppreference.com/w/c/string/byte/strtok
   which is 
    "licensed under Creative Commons Attribution-Sharealike 3.0
     Unported License (CC-BY-SA) and by the GNU Free Documentation License
     (GFDL) (unversioned, with no invariant sections, front-cover texts, or
     back-cover texts). That means that you can use this site in almost any way
     you like, including mirroring, copying, translating, etc. All we would ask
     is to provide link back to cppreference.com so that people know where to
     get the most up-to-date content. In addition to that, any modified content
     should be released under an equivalent license so that everyone could
     benefit from the modified versions. "  */

/* { dg-additional-options " -Wno-analyzer-too-complex -Wno-analyzer-symbol-too-complex" } */

#define __STDC_WANT_LIB_EXT1__ 0
#include <string.h>
#include <stdio.h>
 
int main(void)
{
    char input[] = "A bird came down the walk";
    printf("Parsing the input string '%s'\n", input);
    char *token = strtok(input, " ");
    while(token) {
        puts(token);
        token = strtok(NULL, " ");
    }
 
    printf("Contents of the input string now: '");
    for(size_t n = 0; n < sizeof input; ++n)
        input[n] ? putchar(input[n]) : fputs("\\0", stdout);
    puts("'");
 
#ifdef __STDC_LIB_EXT1__
    char str[] = "A bird came down the walk";
    rsize_t strmax = sizeof str;
    const char *delim = " ";
    char *next_token;
    printf("Parsing the input string '%s'\n", str);
    token = strtok_s(str, &strmax, delim, &next_token);
    while(token) {
        puts(token);
        token = strtok_s(NULL, &strmax, delim, &next_token);
    }
 
    printf("Contents of the input string now: '");
    for(size_t n = 0; n < sizeof str; ++n)
        str[n] ? putchar(str[n]) : fputs("\\0", stdout);
    puts("'");
#endif
}
