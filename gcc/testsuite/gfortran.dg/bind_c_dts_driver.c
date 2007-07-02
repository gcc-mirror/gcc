double fabs (double);

/* interops with myftype_1 */
typedef struct {
   int m, n;
   float r;
} myctype_t;

/* interops with particle in f90 */
typedef struct particle
{
   double x;  /* x position */
   double vx; /* velocity in x direction */
   double y;  /* y position */
   double vy; /* velocity in y direction */
   double z;  /* z position */
   double vz; /* velocity in z direction */
   double m;  /* mass */
}particle_t;

extern void abort(void);
void types_test(particle_t *my_particles, int num_particles);
/* declared in the fortran module bind_c_dts */
extern myctype_t myDerived;

int main(int argc, char **argv)
{
   particle_t my_particles[100];

   /* the fortran code will modify the middle particle */
   my_particles[49].x = 1.0;
   my_particles[49].vx = 1.0;
   my_particles[49].y = 1.0;
   my_particles[49].vy = 1.0;
   my_particles[49].z = 1.0;
   my_particles[49].vz = 1.0;
   my_particles[49].m = 1.0;

   myDerived.m = 1;
   myDerived.n = 2;
   myDerived.r = 3.0;

   types_test(&(my_particles[0]), 100);

   if(fabs(my_particles[49].x - 1.2) > 0.00000000)
      abort();
   if(fabs(my_particles[49].vx - 1.2) > 0.00000000)
      abort();
   if(fabs(my_particles[49].y - 1.2) > 0.00000000)
      abort();
   if(fabs(my_particles[49].vy - 1.2) > 0.00000000)
      abort();
   if(fabs(my_particles[49].z - 1.2) > 0.00000000)
      abort();
   if(fabs(my_particles[49].vz - 1.2) > 0.00000000)
      abort();
   if(fabs(my_particles[49].m - 1.2) > 0.00000000)
      abort();
   if(myDerived.m != 2)
      abort();
   if(myDerived.n != 3)
      abort();
   if(fabs(myDerived.r - 4.0) > 0.00000000)
      abort();
   return 0;
}/* end main() */
