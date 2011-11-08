/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O3" } */

/* The function calculateCircumCircle() should get inlined into the TM
   clone for TMelement_alloc(), so we don't need to generate a TM
   clone for calculateCircumCircle().  We also don't need to put its
   entry into the clone table since it's static.  */

/* { dg-final { scan-assembler-not "ZGTt21calculateCircumCircle" } } */

extern double sqrt(double) __attribute__((transaction_pure));
extern void *xmalloc(int) __attribute__((transaction_safe));

typedef struct coordinate {
    double x;
    double y;
} coordinate_t;
typedef struct element {
    coordinate_t coordinates[3];
    long numCoordinate;
    coordinate_t circumCenter;
    double circumRadius;
} element_t;

__attribute__((transaction_safe))
double
coordinate_distance (coordinate_t* coordinatePtr, coordinate_t* aPtr)
{
    return sqrt( coordinatePtr->x );
}

__attribute__((transaction_safe))
static void
calculateCircumCircle (element_t* elementPtr)
{
    long numCoordinate = elementPtr->numCoordinate;
    coordinate_t* coordinates = elementPtr->coordinates;
    coordinate_t* circumCenterPtr = &elementPtr->circumCenter;
    ((void) (0));
    if (numCoordinate == 2) {
	circumCenterPtr->x = (coordinates[0].x + coordinates[1].x) / 2.0;
	circumCenterPtr->y = (coordinates[0].y + coordinates[1].y) / 2.0;
    }
 else {
	double ax = coordinates[0].x;
	double ay = coordinates[0].y;
	double bx = coordinates[1].x;
	double by = coordinates[1].y;
	double cx = coordinates[2].x;
	double cy = coordinates[2].y;
	double bxDelta = bx - ax;
	double byDelta = by - ay;
	double cxDelta = cx - ax;
	double cyDelta = cy - ay;
	double bDistance2 = (bxDelta * bxDelta) + (byDelta * byDelta);
	double cDistance2 = (cxDelta * cxDelta) + (cyDelta * cyDelta);
	double xNumerator = (byDelta * cDistance2) - (cyDelta * bDistance2);
	double yNumerator = (bxDelta * cDistance2) - (cxDelta * bDistance2);
	double denominator = 2 * ((bxDelta * cyDelta) - (cxDelta * byDelta));
	double rx = ax - (xNumerator / denominator);
	double ry = ay + (yNumerator / denominator);
	circumCenterPtr->x = rx;
	circumCenterPtr->y = ry;
    }
    elementPtr->circumRadius = coordinate_distance(circumCenterPtr,
						   &coordinates[0]);
}

element_t*
element_alloc (coordinate_t* coordinates, long numCoordinate)
{
    element_t* elementPtr;
    elementPtr = (element_t*)xmalloc(sizeof(element_t));
    if (elementPtr) {
	calculateCircumCircle(elementPtr);
    }
    return elementPtr;
}

__attribute__((transaction_safe))
element_t*
TMelement_alloc (coordinate_t* coordinates, long numCoordinate)
{
    element_t* elementPtr;
    elementPtr = (element_t*)xmalloc(sizeof(element_t));
    if (elementPtr) {
	calculateCircumCircle(elementPtr);
    }
    return elementPtr;
}
