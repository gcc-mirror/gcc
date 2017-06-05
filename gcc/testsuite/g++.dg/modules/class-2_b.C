import One;

int z = sizeof (Bob::X); // { dg-error "not a member of .Bob." }
