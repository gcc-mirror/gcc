/*
TEST_OUTPUT:
---
fail_compilation/ice2843.d(22): Error: incompatible types for ((1) is (typeid(int))): 'int' and 'object.TypeInfo'
---
*/

// Issue 2843 - ICE(constfold.c) with is-expression with invalid dot-expression in is-expression involving typeid expression

/* 2843 Assertion failure: '0' on line 863 in file 'constfold.c'
PATCH: constfold.c, line 861:
OLD:
        }else
        assert(0);
NEW:
        }else if (e1->isConst() && e2->isConst()) {
        // Comparing a SymExp with a literal, eg typeid(int) is 7.1;
           cmp=0; // An error has already occurred. Prevent an ICE.
        }else
        assert(0);
*/
bool b = 1 is typeid(int);
