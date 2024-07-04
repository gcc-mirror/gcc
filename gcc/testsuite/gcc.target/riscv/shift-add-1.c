/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

int composeFromSurrogate(const unsigned short high) {

        return  ((high - 0xD800) << 10) ;
}


long composeFromSurrogate_2(const unsigned long high) {

        return  ((high - 0xD800) << 10) ;
}


/* { dg-final { scan-assembler-times "\tli\t" 2 } } */
/* { dg-final { scan-assembler-times "\tslli\t" 2 } } */
/* { dg-final { scan-assembler-times "\taddw\t" 1 } } */
/* { dg-final { scan-assembler-times "\tadd\t" 1 } } */

