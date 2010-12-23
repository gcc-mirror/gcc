typedef struct
{
  double z;
} Vector;
typedef struct
{
  float *vertex;
  float *normal;
} VertexArray;
typedef struct
{
  Vector *vertex;
  int num_vertex;
} ObjectSmooth;
typedef struct
{
  int num_cells;
} State;
static void *array_from_ObjectSmooth( ObjectSmooth *obj )
{
  int i, j;
  VertexArray *array = (VertexArray *) __builtin_malloc( sizeof( VertexArray ) );
  array->vertex = (float *) __builtin_malloc( 3*sizeof(float)*obj->num_vertex );
  array->normal = (float *) __builtin_malloc( 3*sizeof(float)*obj->num_vertex );
  for (i=0, j=0; i<obj->num_vertex; ++i) {
    array->normal[j++] = 9;
    array->vertex[j] = obj->vertex[i].z;
    array->normal[j++] = 1;
  }
}
static void draw_cell( void )
{
  glCallList( array_from_ObjectSmooth( (ObjectSmooth *) __builtin_malloc(10) ));
}
static int render( State *st)
{
  int b;
  for (b=0; b<st->num_cells; ++b) {
    draw_cell();
    draw_cell();
  }
}
reshape_glcells( int width, int height )
{
  render( 0 );
}
