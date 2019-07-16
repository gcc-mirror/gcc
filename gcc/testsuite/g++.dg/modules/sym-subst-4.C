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

// { dg-final { scan-assembler {_ZW4zero4eins4zwei4drei4vier4funf5sechs6sieben4acht4neunE4frobRW_9E6mytype:} } }
