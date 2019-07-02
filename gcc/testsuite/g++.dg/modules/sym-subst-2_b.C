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

// { dg-final { scan-assembler {_ZW3bob5kevinE3barRWW0_E6mytype:} } }
// { dg-final { scan-assembler {_ZW3bob6stuartE5innerIWW_5kevinE6mytypeEvRT_:} } }
