// { dg-do compile }
// { dg-options "-fgnu-tm" }

int x = 0;
int inc_func(int i) {
     for (int j = 0; j < i; ++j)
     {
         __transaction_atomic { x+=1; }
     }
     return 0;
}

// { dg-final { scan-assembler "ITM_commitTransaction" } }
