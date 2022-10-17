// https://issues.dlang.org/show_bug.cgi?id=21591

alias F = void function();

void fn1(void function(), void function());
void fr1(F, F);
static assert(fn1.mangleof == "_D9test215913fn1FPFZvQeZv");
static assert(fr1.mangleof == "_D9test215913fr1FPFZvQeZv");

void fn2(void function()*, void function()*);
void fr2(F*, F*);
static assert(fn2.mangleof == "_D9test215913fn2FPPFZvQfZv");
static assert(fr2.mangleof == "_D9test215913fr2FPPFZvQfZv");

void function() fn3(void function()**, void function()*);
F fr3(F**, F*);
static assert(fn3.mangleof == "_D9test215913fn3FPPPFZvQfZQh");
static assert(fr3.mangleof == "_D9test215913fr3FPPPFZvQfZQh");

void function()** fn4(ref void function(), ref void function()*);
F** fr4(ref F, ref F*);
static assert(fn4.mangleof == "_D9test215913fn4FKPFZvKPQgZPQf");
static assert(fr4.mangleof == "_D9test215913fr4FKPFZvKPQgZPQf");


alias D = void delegate();

void dg1(void delegate(), void delegate());
void dr1(D, D);
static assert(dg1.mangleof == "_D9test215913dg1FDFZvQeZv");
static assert(dr1.mangleof == "_D9test215913dr1FDFZvQeZv");

void dg2(void delegate()*, void delegate()*);
void dr2(D*, D*);
static assert(dg2.mangleof == "_D9test215913dg2FPDFZvQfZv");
static assert(dr2.mangleof == "_D9test215913dr2FPDFZvQfZv");

void delegate() dg3(void delegate()**, void delegate()*);
D dr3(D**, D*);
static assert(dg3.mangleof == "_D9test215913dg3FPPDFZvQfZQh");
static assert(dr3.mangleof == "_D9test215913dr3FPPDFZvQfZQh");

void delegate()** dg4(ref void delegate(), ref void delegate()*);
D** dr4(ref D, ref D*);
static assert(dg4.mangleof == "_D9test215913dg4FKDFZvKPQgZPQf");
static assert(dr4.mangleof == "_D9test215913dr4FKDFZvKPQgZPQf");
