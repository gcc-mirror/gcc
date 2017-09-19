// { dg-do assemble  }

static union {
  union {
  };
}; // { dg-error "" } anonymous union with no members
