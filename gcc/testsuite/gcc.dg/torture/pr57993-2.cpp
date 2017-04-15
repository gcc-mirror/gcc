/* This ICEd due to an incomplete fix for PR57993.  */
/* { dg-do compile } */

extern "C"
{
  extern double sqrt (double __x) throw ();
  typedef long unsigned int size_t;
  typedef struct
  {
  }
  __mbstate_t;
  void *pov_malloc (size_t size, const char *file, int line, const char *msg);
  typedef struct Object_Struct OBJECT;
  typedef struct Ray_Struct RAY;
  typedef struct istack_struct ISTACK;
  typedef struct istk_entry INTERSECTION;
  typedef double UV_VECT[2];
  typedef double VECTOR[3];
  typedef struct Transform_Struct TRANSFORM;
  typedef struct Method_Struct METHODS;
  typedef int (*ALL_INTERSECTIONS_METHOD) (OBJECT *, RAY *, ISTACK *);
  typedef int (*INSIDE_METHOD) (VECTOR, OBJECT *);
  typedef void (*NORMAL_METHOD) (VECTOR, OBJECT *, INTERSECTION *);
  typedef void (*UVCOORD_METHOD) (UV_VECT, OBJECT *, INTERSECTION *);
  typedef void *(*COPY_METHOD) (OBJECT *);
  typedef void (*TRANSLATE_METHOD) (OBJECT *, VECTOR, TRANSFORM *);
  typedef void (*ROTATE_METHOD) (OBJECT *, VECTOR, TRANSFORM *);
  typedef void (*SCALE_METHOD) (OBJECT *, VECTOR, TRANSFORM *);
  typedef void (*TRANSFORM_METHOD) (OBJECT *, TRANSFORM *);
  typedef void (*INVERT_METHOD) (OBJECT *);
  typedef void (*DESTROY_METHOD) (OBJECT *);
  struct Method_Struct
  {
    ALL_INTERSECTIONS_METHOD All_Intersections_Method;
    INSIDE_METHOD Inside_Method;
    NORMAL_METHOD Normal_Method;
    UVCOORD_METHOD UVCoord_Method;
    COPY_METHOD Copy_Method;
    TRANSLATE_METHOD Translate_Method;
    ROTATE_METHOD Rotate_Method;
    SCALE_METHOD Scale_Method;
    TRANSFORM_METHOD Transform_Method;
    INVERT_METHOD Invert_Method;
    DESTROY_METHOD Destroy_Method;
  };
  typedef struct Bicubic_Patch_Struct BICUBIC_PATCH;
  typedef struct Bezier_Node_Struct BEZIER_NODE;
  struct Bezier_Node_Struct
  {
    int Node_Type;
    int Count;
  };
  struct Bicubic_Patch_Struct
  {
    METHODS *Methods;
    int Patch_Type, U_Steps, V_Steps;
    VECTOR Control_Points[4][4];
    BEZIER_NODE *Node_Tree;
  };
  typedef enum
  {
    CSV, SYS, PPM, TARGA, PNG, NONE
  }
  SHELLDATA;
  typedef enum STATS
  {
    Number_Of_Pixels =
      0, Number_Of_Pixels_Supersampled, Number_Of_Samples, Number_Of_Rays,
      Calls_To_DNoise, Calls_To_Noise, ADC_Saves, Istack_overflows,
      Ray_RBezier_Tests, Ray_RBezier_Tests_Succeeded, Ray_Bicubic_Tests,
      Ray_Bicubic_Tests_Succeeded, Ray_Blob_Tests, Ray_Blob_Tests_Succeeded,
      Blob_Element_Tests, Blob_Element_Tests_Succeeded, Blob_Bound_Tests,
      Blob_Bound_Tests_Succeeded, Ray_Box_Tests, Ray_Box_Tests_Succeeded,
      Ray_Cone_Tests, Ray_Cone_Tests_Succeeded, Ray_CSG_Intersection_Tests,
      Ray_CSG_Intersection_Tests_Succeeded, Ray_CSG_Merge_Tests,
      Ray_CSG_Merge_Tests_Succeeded, Ray_CSG_Union_Tests,
      Ray_CSG_Union_Tests_Succeeded, Ray_Disc_Tests, Ray_Disc_Tests_Succeeded,
      Ray_Fractal_Tests, Ray_Fractal_Tests_Succeeded, Ray_HField_Tests,
      Ray_HField_Tests_Succeeded, Ray_HField_Box_Tests,
      Ray_HField_Box_Tests_Succeeded, Ray_HField_Triangle_Tests,
      Ray_HField_Triangle_Tests_Succeeded, Ray_HField_Block_Tests,
      Ray_HField_Block_Tests_Succeeded, Ray_HField_Cell_Tests,
      Ray_HField_Cell_Tests_Succeeded, Ray_IsoSurface_Tests,
      Ray_IsoSurface_Tests_Succeeded, Ray_IsoSurface_Bound_Tests,
      Ray_IsoSurface_Bound_Tests_Succeeded, Ray_IsoSurface_Cache,
      Ray_IsoSurface_Cache_Succeeded, Ray_Lathe_Tests,
      Ray_Lathe_Tests_Succeeded, Lathe_Bound_Tests,
      Lathe_Bound_Tests_Succeeded, Ray_Mesh_Tests, Ray_Mesh_Tests_Succeeded,
      Ray_Plane_Tests, Ray_Plane_Tests_Succeeded, Ray_Polygon_Tests,
      Ray_Polygon_Tests_Succeeded, Ray_Prism_Tests, Ray_Prism_Tests_Succeeded,
      Prism_Bound_Tests, Prism_Bound_Tests_Succeeded, Ray_Parametric_Tests,
      Ray_Parametric_Tests_Succeeded, Ray_Par_Bound_Tests,
      Ray_Par_Bound_Tests_Succeeded, Ray_Quadric_Tests,
      Ray_Quadric_Tests_Succeeded, Ray_Poly_Tests, Ray_Poly_Tests_Succeeded,
      Ray_Sphere_Tests, Ray_Sphere_Tests_Succeeded, Ray_Sphere_Sweep_Tests,
      Ray_Sphere_Sweep_Tests_Succeeded, Ray_Superellipsoid_Tests,
      Ray_Superellipsoid_Tests_Succeeded, Ray_Sor_Tests,
      Ray_Sor_Tests_Succeeded, Sor_Bound_Tests, Sor_Bound_Tests_Succeeded,
      Ray_Torus_Tests, Ray_Torus_Tests_Succeeded, Torus_Bound_Tests,
      Torus_Bound_Tests_Succeeded, Ray_Triangle_Tests,
      Ray_Triangle_Tests_Succeeded, Ray_TTF_Tests, Ray_TTF_Tests_Succeeded,
      Bounding_Region_Tests, Bounding_Region_Tests_Succeeded,
      Clipping_Region_Tests, Clipping_Region_Tests_Succeeded,
      Ray_IsoSurface_Find_Root, Ray_Function_VM_Calls,
      Ray_Function_VM_Instruction_Est, VBuffer_Tests, VBuffer_Tests_Succeeded,
      LBuffer_Tests, LBuffer_Tests_Succeeded, Media_Samples, Media_Intervals,
      Reflected_Rays_Traced, Refracted_Rays_Traced, Transmitted_Rays_Traced,
      Internal_Reflected_Rays_Traced, Shadow_Cache_Hits,
      Shadow_Rays_Succeeded, Shadow_Ray_Tests, nChecked, nEnqueued,
      totalQueues, totalQueueResets, totalQueueResizes, Polynomials_Tested,
      Roots_Eliminated, MemStat_Smallest_Alloc, MemStat_Largest_Alloc,
      MemStat_Largest_Mem_Usage, Number_Of_Photons_Shot,
      Number_Of_Photons_Stored, Number_Of_Global_Photons_Stored,
      Number_Of_Media_Photons_Stored, Priority_Queue_Add,
      Priority_Queue_Remove, Gather_Performed_Count, Gather_Expanded_Count,
      MaxStat
  }
  Stats;
  static int All_Bicubic_Patch_Intersections (OBJECT * Object, RAY * Ray,
					      ISTACK * Depth_Stack);
  static int Inside_Bicubic_Patch (VECTOR IPoint, OBJECT * Object);
  static void Bicubic_Patch_Normal (VECTOR Result, OBJECT * Object,
				    INTERSECTION * Inter);
  static void Bicubic_Patch_UVCoord (UV_VECT Result, OBJECT * Object,
				     INTERSECTION * Inter);
  static BICUBIC_PATCH *Copy_Bicubic_Patch (OBJECT * Object);
  static void Translate_Bicubic_Patch (OBJECT * Object, VECTOR Vector,
				       TRANSFORM * Trans);
  static void Rotate_Bicubic_Patch (OBJECT * Object, VECTOR Vector,
				    TRANSFORM * Trans);
  static void Scale_Bicubic_Patch (OBJECT * Object, VECTOR Vector,
				   TRANSFORM * Trans);
  static void Transform_Bicubic_Patch (OBJECT * Object, TRANSFORM * Trans);
  static void Invert_Bicubic_Patch (OBJECT * Object);
  static void Destroy_Bicubic_Patch (OBJECT * Object);
  static METHODS Bicubic_Patch_Methods = {
    All_Bicubic_Patch_Intersections, Inside_Bicubic_Patch,
      Bicubic_Patch_Normal, Bicubic_Patch_UVCoord,
      (COPY_METHOD) Copy_Bicubic_Patch, Translate_Bicubic_Patch,
      Rotate_Bicubic_Patch, Scale_Bicubic_Patch, Transform_Bicubic_Patch,
      Invert_Bicubic_Patch, Destroy_Bicubic_Patch
  };
  static void bezier_value (VECTOR (*Control_Points)[4][4], double u0,
			    double v0, VECTOR P, VECTOR N)
  {
    int i, j;
    double c, t, ut, vt;
    double u[4], uu[4], v[4], vv[4];
    double du[4], duu[4], dv[4], dvv[4];
    for (i = 1; i < 4; i++)
      {
	vv[i] = vv[i - 1] * (1.0 - v0);
	dvv[i] = -i * vv[i - 1];
      }
    for (i = 0; i < 4; i++)
      {
	for (j = 0; j < 4; j++)
	  {
	    t = c * ut * (dv[j] * vv[3 - j] + v[j] * dvv[3 - j]);
	  }
	t = 1.0 / sqrt (t);
      }
  }
  static int intersect_subpatch (BICUBIC_PATCH * Shape, RAY * ray,
				 VECTOR V1[3], double uu[3], double vv[3],
				 double *Depth, VECTOR P, VECTOR N, double *u,
				 double *v)
  {
    VECTOR Q, T1;
    VECTOR B[3], IB[3], NN[3];
    bezier_value ((VECTOR (*)[4][4]) & Shape->Control_Points, uu[1], vv[1],
		  T1, NN[1]);
  }
  static int bezier_tree_walker (RAY * Ray, BICUBIC_PATCH * Shape,
				 BEZIER_NODE * Node, ISTACK * Depth_Stack)
  {
    int i, cnt = 0;
    double Depth, u, v;
    double uu[3], vv[3];
    VECTOR N, P;
    VECTOR V1[3];
    if (Node->Node_Type == 0)
      {
	for (i = 0; i < Node->Count; i++)
	  {
	  }
	if (intersect_subpatch (Shape, Ray, V1, uu, vv, &Depth, P, N, &u, &v))
	  {
	  }
      }
  }
  static int All_Bicubic_Patch_Intersections (OBJECT * Object, RAY * Ray,
					      ISTACK * Depth_Stack)
  {
    int Found, cnt = 0;
    switch (((BICUBIC_PATCH *) Object)->Patch_Type)
      {
      case 1:
	cnt =
	  bezier_tree_walker (Ray, (BICUBIC_PATCH *) Object,
			      ((BICUBIC_PATCH *) Object)->Node_Tree,
			      Depth_Stack);
      }
  }
  BICUBIC_PATCH *Create_Bicubic_Patch ()
  {
    BICUBIC_PATCH *New;
    New =
      (BICUBIC_PATCH *) pov_malloc ((sizeof (BICUBIC_PATCH)), "bezier.cpp",
				    2079, ("bicubic patch"));
    New->Methods = &Bicubic_Patch_Methods;
  }
}
