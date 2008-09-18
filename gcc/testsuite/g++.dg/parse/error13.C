// { dg-options "-fshow-column" }
// PR c++/13975

public: // { dg-error "1:expected unqualified-id before 'public'" }

int i;

protected: // { dg-error "1:expected unqualified-id before 'protected'" }

int j;

private: // { dg-error "1:expected unqualified-id before 'private'" }

int k;
