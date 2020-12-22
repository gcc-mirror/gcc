// { dg-additional-options "-fmodules-ts" }
export module bob.stuart.kevin;
// { dg-module-cmi bob.stuart.kevin }

class mytype 
{
};

void frob (mytype &)
{
}

// { dg-final { scan-assembler {_ZW3bob6stuart5kevinE4frobRW_2E6mytype:} } }
