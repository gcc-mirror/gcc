

import thing.baz;

module thing.baz [[interface]]; // { dg-error "already imported" }

import thing.baz;
