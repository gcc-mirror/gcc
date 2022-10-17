// https://issues.dlang.org/show_bug.cgi?id=21177
/*
DISABLED: win
TEST_OUTPUT:
---
compilable/test21177.d(103): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(111): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(150): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(151): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(152): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(153): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(154): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(155): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(202): Deprecation: format specifier `"%m"` is invalid
compilable/test21177.d(203): Deprecation: argument `d` for format specification `"%mc"` must be `char**`, not `int`
compilable/test21177.d(204): Deprecation: argument `c` for format specification `"%ms"` must be `char**`, not `char*`
compilable/test21177.d(205): Deprecation: format specifier `"%ml"` is invalid
compilable/test21177.d(206): Deprecation: argument `d` for format specification `"%mlc"` must be `wchar_t**`, not `int`
compilable/test21177.d(207): Deprecation: argument `c` for format specification `"%mls"` must be `wchar_t**`, not `char*`
---
*/

import core.stdc.stdio;
import core.stdc.string;
import core.stdc.stdlib;

void main()
{
    version (CRuntime_Glibc)
    {
        #line 100
        printf("%m this is a string in errno");
        printf("%s %m", "str".ptr, 2);
        printf("%m %a", 2.);
        printf("%m %m %s");
        printf("%m");
        printf("%*m");
        pragma(msg, "compilable/test21177.d(111): Deprecation: more format specifiers than 0 arguments");
    }
    else
    {
        pragma(msg, "compilable/test21177.d(103): Deprecation: more format specifiers than 0 arguments");
        printf("%m");
    }
    {
        char* a, b;
        sscanf("salut poilu", "%ms %m[^\n]", &a, &b);
        assert(!strcmp(a, b));
        free(a);
        free(b);

        char* t; wchar_t* p;
        sscanf("Tomate Patate", "%mc %mlc", &t, &p);
        free(t);
        free(p);

        #line 150
        sscanf("150", "%m");
        sscanf("151", "%ms");
        sscanf("152", "%mc");
        sscanf("153", "%ml");
        sscanf("154", "%mls");
        sscanf("155", "%mlc");

        #line 200
        char* c;
        int d;
        sscanf("204", "%m", c);
        sscanf("205", "%mc", d);
        sscanf("206", "%ms", c);
        sscanf("207", "%ml", d);
        sscanf("208", "%mlc", d);
        sscanf("209", "%mls", c);
    }
}
