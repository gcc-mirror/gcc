// https://issues.dlang.org/show_bug.cgi?id=23826

// REQUIRED_ARGS: -de

/*
TEST_OUTPUT:
---
fail_compilation/fail23826.d(23): Deprecation: alias `fail23826.S.value` is deprecated
---
*/

alias Alias(alias A) = A;

class S
{
    deprecated alias value = Alias!5;
}

enum identity(alias A) = A;

void main()
{
    auto a = identity!(S.value);
}
