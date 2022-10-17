module issue12520;

// https://issues.dlang.org/show_bug.cgi?id=12520

alias Seq(T...) = T;

static assert( Seq!()  == Seq!()            );
static assert( Seq!(1) == Seq!(1)           );

static assert((Seq!()  != Seq!())   == false);
static assert((Seq!(1) != Seq!(1))  == false);

static assert( Seq!() != Seq!(1)            );
static assert( Seq!(1) != Seq!()            );
static assert( Seq!(0) != Seq!(1)           );
static assert( Seq!(0,1) != Seq!()          );

static assert((Seq!() == Seq!(1))   == false);
static assert((Seq!(1) == Seq!())   == false);
static assert((Seq!(0) == Seq!(1))  == false);
static assert((Seq!(0,1) == Seq!()) == false);
