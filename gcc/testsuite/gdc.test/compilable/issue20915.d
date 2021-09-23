module issue20915;

// prior to the PR adding this test case,
// locally defined version and debug idents were included.
version = illegal;
debug   = illegal;

alias Seq(T...) = T;

static assert (__traits(allMembers, issue20915) == Seq!("object", "Seq"));
