// REQUIRED_ARGS: -unittest -boundscheck=off
// PERMUTE_ARGS:
// EXTRA_SOURCES: imports/a15030.d imports/b15030.d
// EXTRA_FILES: imports/std15030algo.d

void main() {}

/+
Compiler output with -v switch.

With 2.068.0:
--------
code      a
code      b
function  b.__unittestL5_2
function  b.__unittestL5_2.__lambda1
function  b.__unittestL5_2.__lambda1.__lambda2
function  b.__unittestL5_2.__lambda1.__lambda2.filter!((a) => a).filter!(int[]).filter
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.this
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.empty
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.front
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.popFront
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.__xopEquals
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.__xtoHash
function  b.__unittestL5_2.__lambda1.__lambda2.__lambda2

The nested functions '__lambda1', '__lambda2', and 'filter' are
(fortunately) generated from outer to inner.


With 2.068.1:
--------
code      a
function  b.__unittestL5_2.__lambda1.__lambda2.filter!((a) => a).filter!(int[]).filter
function  b.__unittestL5_2.__lambda1.__lambda2
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.this
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.empty
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.front
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.popFront
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.__xopEquals
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.__xtoHash
code      b
function  b.__unittestL5_2
function  b.__unittestL5_2.__lambda1
Assertion failure: '!v->csym' on line 1060 in file 'glue.c'

abnormal program termination

'filer' is generated before its ancestor functions '__lambda1' and '__lambda2' - it's a bug.


Fixed (contains debug prints):
--------
code      a
b.__unittestL5_2.__lambda1.__lambda2.filter!((a) => a).filter!(int[]).filter @[algorithm.d(5)]
    --> pushed to unittest @[b.d(5)]
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.this
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.empty
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.front
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.popFront
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.__xopEquals
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.__xtoHash
code      b
function  b.__unittestL5_2
function  b.__unittestL5_2.__lambda1
function  b.__unittestL5_2.__lambda1.__lambda2
function  b.__unittestL5_2.__lambda1.__lambda2.filter!((a) => a).filter!(int[]).filter
function  b.__unittestL5_2.__lambda1.__lambda2.__lambda2

By using `deferredNested` correctly, those nested function generations are ordered again.


Fixed more:
--------
function  D main
code      a
code      b
function  b.__unittestL5_2
function  b.__unittestL5_2.__lambda1
function  b.__unittestL5_2.__lambda1.__lambda2
function  b.__unittestL5_2.__lambda1.__lambda2.filter!(int[]).filter
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.this
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.empty
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.front
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.popFront
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.__xopEquals
function  b.__unittestL5_2.__lambda1.__lambda2.FilterResult!(__lambda2, int[]).FilterResult.__xtoHash
function  b.__unittestL5_2.__lambda1.__lambda2.__lambda2

By the tweak in TemplateInstance::appendToModuleMember(), all the code of
(implicitly) nested instances are stored into corresponding module object file.

+/
