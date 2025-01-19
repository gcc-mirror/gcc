/* This used to not be allowed
 * https://github.com/dlang/dmd/pull/20634
 */

struct A
{
    this (ref shared A a) immutable {}
}

struct B
{
    A a;
    this(immutable B b) shared {}
}
