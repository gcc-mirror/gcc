// { dg-additional-options "-fmodules-ts" }

export module bob.kevin;
// { dg-module-cmi bob.kevin }

import bob.stuart;

class mytype 
{
};

void bar (mytype &m)
{
  foo (m);
}

// { dg-final { scan-assembler {_ZW3bob5kevinE3barRW_1E6mytype:} } }
// { dg-final { scan-assembler {_ZW3bob6stuartE5innerIW_05kevinE6mytypeEvRT_:} } }
