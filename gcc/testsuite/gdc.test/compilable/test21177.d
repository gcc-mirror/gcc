// https://issues.dlang.org/show_bug.cgi?id=21177
/*
DISABLED: win
TEST_OUTPUT:
---
compilable/test21177.d(103): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(150): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(151): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(152): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(153): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(200): Deprecation: more format specifiers than 0 arguments
compilable/test21177.d(203): Deprecation: format specifier `"%m"` is invalid
compilable/test21177.d(204): Deprecation: format specifier `"%m"` is invalid
compilable/test21177.d(205): Deprecation: argument `c` for format specification `"%a"` must be `float*`, not `char*`
compilable/test21177.d(206): Deprecation: argument `c` for format specification `"%a"` must be `float*`, not `char*`
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
        printf("%a", 2.);
        printf("%m %m %s");
        printf("%*m");

        char* a, b;
        sscanf("salut poilu", "%a %m", a, b);
        assert(!strcmp(a, b));
        free(a);
        free(b);

        char* t, p;
        sscanf("Tomate Patate", "%ms %as", t, p);
        free(t);
        free(p);

        #line 150
        sscanf("150", "%m");
        sscanf("151", "%ms");
        sscanf("152", "%a");
        sscanf("153", "%as");

        pragma(msg, "compilable/test21177.d(200): Deprecation: more format specifiers than 0 arguments");
        pragma(msg, "compilable/test21177.d(203): Deprecation: format specifier `\"%m\"` is invalid");
        pragma(msg, "compilable/test21177.d(204): Deprecation: format specifier `\"%m\"` is invalid");
        pragma(msg, "compilable/test21177.d(205): Deprecation: argument `c` for format specification `\"%a\"` must be `float*`, not `char*`");
        pragma(msg, "compilable/test21177.d(206): Deprecation: argument `c` for format specification `\"%a\"` must be `float*`, not `char*`");
    }
    else
    {
        // fake it
        pragma(msg, "compilable/test21177.d(103): Deprecation: more format specifiers than 0 arguments");
        pragma(msg, "compilable/test21177.d(150): Deprecation: more format specifiers than 0 arguments");
        pragma(msg, "compilable/test21177.d(151): Deprecation: more format specifiers than 0 arguments");
        pragma(msg, "compilable/test21177.d(152): Deprecation: more format specifiers than 0 arguments");
        pragma(msg, "compilable/test21177.d(153): Deprecation: more format specifiers than 0 arguments");

        #line 200
        printf("%m");

        char* c;
        sscanf("204", "%m", c);
        sscanf("205", "%ms", c);
        sscanf("206", "%a", c);
        sscanf("207", "%as", c);

    }
}
