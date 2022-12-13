/* RUN_OUTPUT:
---
value: -5
value: -5
---
*/

// https://issues.dlang.org/show_bug.cgi?id=21506

import core.stdc.stdio;
import core.stdc.stdarg;

extern(C++)
{

void print(long a, va_list args){
    vprintf("value: %d\n", args);
}
void proxy0(long a, long b, long c, long d, bool e, ...){
    va_list ap;
    va_start(ap, e);
    print(a, ap);
    va_end(ap);
//  print(a, _argptr);
}
void proxy1(long d, bool e, ...){
    va_list ap;
    va_start(ap, e);
    print(d, ap);
    va_end(ap);
//  print(d, _argptr);
}

}

void main(){
    int var = -5;
    proxy0(1, 2, 3, 4, true, var);
    proxy1(4, true, var);
}
