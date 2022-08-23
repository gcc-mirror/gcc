// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=106563
// { dg-do link }
// { dg-additional-files "imports/pr106563math.d imports/pr106563regex.d imports/pr106563uni.d" }
// { dg-additional-options "-I[srcdir] -fno-druntime" }
import imports.pr106563math;
import imports.pr106563regex;

auto requireSize()(size_t size)
{
    return nextPow2(size);
}

extern(C) int main()
{
    return cast(int)requireSize(0);
}
