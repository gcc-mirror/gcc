// { dg-do assemble  }

static union {
  union {
  };
}; // { dg-warning "" } anonymous union with no members
