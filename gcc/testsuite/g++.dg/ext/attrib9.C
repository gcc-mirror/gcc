class __attribute__((unused)) C;	//  { dg-warning "type attributes" }
struct __attribute__((unused)) S;	//  { dg-warning "type attributes" }
union __attribute__((unused)) U;	//  { dg-warning "type attributes" }
enum e {};
enum __attribute__((unused)) e;		//  { dg-warning "type attributes" }
