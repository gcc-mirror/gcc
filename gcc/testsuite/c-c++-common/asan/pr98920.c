/* PR sanitizer/98920 */
/* { dg-do run } */

#include <stdio.h>
#include <sys/types.h>
#if __has_include(<regex.h>)
#include <regex.h>
#endif

int main(void)
{
#ifdef REG_STARTEND
    regex_t r;
    const char s[] = "ban\0ana";
    regmatch_t pmatch[10];
    pmatch[0].rm_so = 0;
    pmatch[0].rm_eo = sizeof(s);
    if (regcomp(&r, "ana", 0))
        return 2;
    if (regexec(&r, s, sizeof(pmatch)/sizeof(pmatch[0]), pmatch, REG_STARTEND)) {
        fprintf(stderr, "failed to match\n");
        regfree(&r);
        return 3;
    }
    regfree(&r);
#endif
    return 0;
}
