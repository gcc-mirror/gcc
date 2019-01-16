// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/test55a.d
// PERMUTE_ARGS: -dw
// REQUIRED_ARGS: -d

public import imports.test55a;

class Queue {
  alias int ListHead;
  Arm a;
}

class MessageQueue : Queue {
}

class Queue2 {
  alias int ListHead;
  Arm2 a;
}

