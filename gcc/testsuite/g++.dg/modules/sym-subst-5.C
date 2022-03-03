// { dg-additional-options "-fmodules-ts" }
export module zero.eins.zwei.drei.vier.funf.sechs.sieben.acht.neun.zehn;
// We're in cologne!
// { dg-module-cmi zero.eins.zwei.drei.vier.funf.sechs.sieben.acht.neun.zehn }

class mytype 
{
};

void frob (mytype &)
{
}

// { dg-final { scan-assembler {_ZW4zeroW4einsW4zweiW4dreiW4vierW4funfW5sechsW6siebenW4achtW4neunW4zehn4frobRS9_6mytype:} } }
