// https://bugzilla.gdcproject.org/show_bug.cgi?id=223
// { dg-do compile }

struct S223
{
    long[8] field;
}

class C223
{
    long[8] field;
}

S223 test223_1();
real test223_2();
string[long[8]] test223_3();
C223 test223_4();
long test223_5();
long[] test223_6();
long[8] test223_7();
C223[8] test223_8();
void delegate() test223_9();

bool test223()
{
    return test223_1() == test223_1() &&
           test223_1() is test223_1() &&
           test223_2() == test223_2() &&
           test223_2() is test223_2() &&
           test223_3() == test223_3() &&
           test223_3() is test223_3() &&
           test223_4() == test223_4() &&
           test223_4() is test223_4() &&
           test223_5() == test223_5() &&
           test223_5() is test223_5() &&
           test223_6() == test223_6() &&
           test223_6() is test223_6() &&
           test223_7()[] is test223_7()[] &&
           test223_8() == test223_8() &&
           test223_8()[] is test223_8()[] &&
           test223_9() == test223_9() &&
           test223_9() is test223_9();
}
