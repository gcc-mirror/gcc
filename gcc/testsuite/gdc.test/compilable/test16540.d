/* REQUIRED_ARGS:
   PERMUTE_ARGS:
 */

// https://issues.dlang.org/show_bug.cgi?id=16540

@safe:

void foo(scope lazy int* f) @nogc {
}

void bar1() @nogc {
    foo(new int(5)); // It does not understand that the new here is wrapped in an invisible delegate
}
