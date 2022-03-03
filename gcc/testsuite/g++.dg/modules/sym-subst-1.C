// { dg-additional-options "-fmodules-ts" }
export module bob.stuart.kevin;
// { dg-module-cmi bob.stuart.kevin }

class mytype 
{
};

void frob (mytype &)
{
}

// { dg-final { scan-assembler {_ZW3bobW6stuartW5kevin4frobRS1_6mytype:} } }
