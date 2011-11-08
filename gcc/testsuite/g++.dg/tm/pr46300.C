// { dg-do compile }
// { dg-options "-fgnu-tm" }

void foo(){
    __transaction_atomic {
	throw 5;
    }
}
