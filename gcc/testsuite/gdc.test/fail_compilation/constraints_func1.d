/*
EXTRA_FILES: imports/constraints.d
TEST_OUTPUT:
---
fail_compilation/constraints_func1.d(79): Error: none of the overloads of template `imports.constraints.test1` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(9):        Candidate is: `test1(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_func1.d(80): Error: none of the overloads of template `imports.constraints.test2` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(10):        Candidate is: `test2(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       !P!T`
fail_compilation/constraints_func1.d(81): Error: none of the overloads of template `imports.constraints.test3` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(11):        Candidate is: `test3(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_func1.d(82): Error: none of the overloads of template `imports.constraints.test4` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(12):        Candidate is: `test4(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_func1.d(83): Error: none of the overloads of template `imports.constraints.test5` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(13):        Candidate is: `test5(T)(T v)`
  with `T = int`
  must satisfy one of the following constraints:
`       N!T
       N!T`
fail_compilation/constraints_func1.d(84): Error: none of the overloads of template `imports.constraints.test6` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(14):        Candidate is: `test6(T)(T v)`
  with `T = int`
  must satisfy one of the following constraints:
`       N!T
       N!T
       !P!T`
fail_compilation/constraints_func1.d(85): Error: none of the overloads of template `imports.constraints.test7` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(15):        Candidate is: `test7(T)(T v)`
  with `T = int`
  must satisfy one of the following constraints:
`       N!T
       N!T`
fail_compilation/constraints_func1.d(86): Error: none of the overloads of template `imports.constraints.test8` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(16):        Candidate is: `test8(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_func1.d(87): Error: none of the overloads of template `imports.constraints.test9` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(17):        Candidate is: `test9(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       !P!T`
fail_compilation/constraints_func1.d(88): Error: none of the overloads of template `imports.constraints.test10` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(18):        Candidate is: `test10(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       !P!T`
fail_compilation/constraints_func1.d(89): Error: none of the overloads of template `imports.constraints.test11` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(19):        Candidate is: `test11(T)(T v)`
  with `T = int`
  must satisfy one of the following constraints:
`       N!T
       !P!T`
fail_compilation/constraints_func1.d(90): Error: none of the overloads of template `imports.constraints.test12` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(20):        Candidate is: `test12(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       !P!T`
fail_compilation/constraints_func1.d(92): Error: none of the overloads of template `imports.constraints.test1` are callable using argument types `!()(int, int)`
fail_compilation/imports/constraints.d(9):        Candidate is: `test1(T)(T v)`
---
*/

void main()
{
    import imports.constraints;

    test1(0);
    test2(0);
    test3(0);
    test4(0);
    test5(0);
    test6(0);
    test7(0);
    test8(0);
    test9(0);
    test10(0);
    test11(0);
    test12(0);

    test1(0, 0);
}
