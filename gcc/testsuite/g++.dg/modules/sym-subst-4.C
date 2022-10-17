// { dg-additional-options "-fmodules-ts" }
export module zero.eins.zwei.drei.vier.funf.sechs.sieben.acht.neun;
// We're in cologne!
// { dg-module-cmi zero.eins.zwei.drei.vier.funf.sechs.sieben.acht.neun }

class mytype 
{
};

void frob (mytype &)
{
}

// { dg-final { scan-assembler {_ZW4zeroW4einsW4zweiW4dreiW4vierW4funfW5sechsW6siebenW4achtW4neun4frobRS8_6mytype:} } }
