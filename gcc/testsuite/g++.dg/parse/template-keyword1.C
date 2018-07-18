// PR c++/81257

template < typename ::template A < int > >; // { dg-error "" }
