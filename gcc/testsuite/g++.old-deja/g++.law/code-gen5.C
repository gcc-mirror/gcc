// GROUPS passed code-generation
// code-gen file
// From: "David" <norman@pi14.arc.umn.edu>
// Date:     Mon, 15 Nov 1993 20:59:14 -0600 (CST)
// Subject:  An error!
// Message-ID: <9311160259.AA03353@pi14.arc.umn.edu>

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <fstream.h>
#include <iostream.h>
#include <math.h>

#define ANSI_C

typedef double VEC ;

class Vector;

class VectorInt 
{
	public:

	/* Nothing public!! Only Vector can use this class */

	private:

	VectorInt( int );
	VectorInt( int, double *, int = 0 );
	VectorInt( const VectorInt & );
	~VectorInt();

	VectorInt *refer();
	void unrefer();
	int count;				/* Number of Vector's refering to me */

	VEC *vec;

	friend class Vector;
	friend class VecElem;
};

class VecElem 
{
	public:
	
	operator double();
	double operator=( double );

	private:

	VecElem( Vector &, int );
	VecElem( const VecElem & );

	Vector &v;
	int row;						/* Row element refers to */

	friend class Vector;
};

class Vector 
{
	public:

	Vector();					// Must be assigned to before used
	Vector( VectorInt * );
	Vector( int );
	Vector( int, double *, int beg = 0 );
	Vector( const Vector & );
	Vector &operator=( const Vector & );
	~Vector() { if(r) r->unrefer(); };

	int row() const { return 19; }
	int dim() const { return 10; }

	double operator()( int ) const;
	VecElem operator()( int );

	double assign( int, double );

	friend ostream& operator<<( ostream&, const Vector& m );
	
	private:

	VectorInt *r;			/* Reference to real data */

	friend class VecElem;
	friend class LUDecom;
	friend class SVD;
};


Vector::
Vector()
	: r(0)
{}

Vector::
Vector( VectorInt *vi )
	: r(vi)
{
	r->refer();
}

Vector::
Vector( int row )
{
	assert( row > 0 );

	r = new VectorInt( row );

	r->refer();
}

Vector::
Vector( int row, double *d, int beg )
{
	assert( row > 0 );

	r = new VectorInt( row, d, beg );

	r->refer();
}

Vector::
Vector( const Vector &A )
	: r( A.r->refer() )
{}

Vector& Vector::
operator=( const Vector &A )
{
	if( r )
		r->unrefer();

	r = A.r->refer();

	return *this;
}

double Vector::
operator()( int row ) const
{
	assert( r );

	return *r->vec;
}

VecElem Vector::
operator()( int r )
{
	assert(r);

	return VecElem( *this, r );
}

	/* assign changes the matrix, it does not create a new one! */
double Vector::
assign( int rownum, double d )
{
	assert(r);

	if( rownum > row() || rownum <= 0 ) {
		cerr << "Warning: trying to assign out of bounds" << endl;
		cerr << "row " << rownum << endl;
		cerr << "Vector size " << row() << endl;
		abort();
	}

	if( r->count == 1 ) {
			/* Don't need to create a new matrix, since we are the only */
			/*  one pointing to ours 									*/
	}
	else {
		VectorInt *vi = new VectorInt( *r );
		r->unrefer();
		r = vi->refer();
	}

	return d;
}


VectorInt::
VectorInt( int sx )
	: vec( new double[sx] ), count(0)
{ }

VectorInt::
VectorInt( int sx, double *, int )
	: vec( new double[sx] ), count(0)
{
}

VectorInt::
VectorInt( const VectorInt & )
	: vec( new double[10] ), count(0)
{
}

VectorInt * VectorInt::
refer()
{
	count ++;
	return this;

	// cout << "Refering vec" << endl;
}

void VectorInt::
unrefer()
{
	count--;

	if( count == 0 ) {
		delete this;
	}

	// cout << "Unrefering vec" << endl;
}

VectorInt::
~VectorInt()
{
	delete vec;
	vec = 0;
}

VecElem::
VecElem( Vector &vec, int r )
	: v(vec), row(r)
{
	if( r < 1 || r > vec.row() ) {
		cerr << "Trying to access vector element out of bounds" << endl;
		abort();
	}
}

VecElem::
VecElem( const VecElem &elem )
	: v(elem.v), row(elem.row)
{}

VecElem::
operator double()
{
	assert( v.r->vec );
	return *v.r->vec;
};

double VecElem::
operator=( double d )
{
	return v.assign( row, d );
}





int makeforms( Vector cen, Vector **a, Vector **b );

int main()
{
	Vector *a[8], *b[8], disp(3);
	Vector cen(3), cen2(3);
	int i, j;

	if (makeforms (cen,a,b) != 10)
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");


}

int
makeforms( Vector cen, Vector **a, Vector **b)
{
	return 10;
}

