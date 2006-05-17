typedef struct atype
{
    float bg[1], cg[1];
    _Bool ant;
}atype;


void cp_assert(_Bool*, float*, int*, _Bool*);

void f(atype **rng_stream, int *error, float u)
{
    _Bool t = *rng_stream != 0;
    float routinep;
    _Bool failure;
    cp_assert ( &t, &routinep, error, &failure);
    if (failure == 0)
    {
        typedef float ty[1];
        ty *tt = &((*rng_stream)->bg);
        int i = 1;

        do 
        {
            (*tt)[i - 1] = u;
            i ++;
        }while (i > 1);
        {
            ty *tt = &(*rng_stream)->cg;
            int i = 1;

            do 
            {
                (*tt)[i - 1] = u;
                i ++;
            }while (i > 1);
        }
    }    
}


