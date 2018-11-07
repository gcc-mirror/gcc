template A(alias B) {}

mixin template C(alias B = cast(NonExistent)null) {
  alias A!B D;
}

mixin C!();

