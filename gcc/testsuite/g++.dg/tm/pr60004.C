// { dg-do compile }
// { dg-options "-fgnu-tm" }

int a;
int f() {
    __transaction_atomic {
        if (a == 5)
            return 1;
    }
}
