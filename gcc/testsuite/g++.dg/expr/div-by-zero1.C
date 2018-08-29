// PR c++/84938

const int &a = 1 / ~-1;		// { dg-warning "division by zero" }
