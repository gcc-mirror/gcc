// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=88957
// { dg-do compile }
// { dg-additional-options "-fsanitize=undefined" }

alias int4 = __vector(int[4]);

int fn(const int[4] x)
{
    int sum = 0;
    foreach (i; x) sum += i;
    return sum;
}

void pr88957()
{
    auto x = fn(int4.init.array);
    auto y = fn(int4(2).array);
}
