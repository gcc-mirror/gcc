// PR c++/47277
// { dg-options -std=c++0x }

int main(void) {
           enum e {};
           e ev;
           ev.e::~e_u();	// { dg-error "" }
}
