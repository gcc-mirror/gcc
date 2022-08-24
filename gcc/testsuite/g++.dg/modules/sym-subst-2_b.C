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

// { dg-final { scan-assembler {_ZW3bobW5kevin3barRS0_6mytype:} } }
// { dg-final { scan-assembler {_ZW3bobW6stuart5innerIS_W5kevin6mytypeEvRT_:} } }
