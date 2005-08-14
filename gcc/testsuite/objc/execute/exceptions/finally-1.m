#include <objc/Object.h>

int
thrower_try_body()
{
        printf("Thrower try body\n");
        return (0);
}

int
finally_body()
{
        printf("Finally body\n");
        return (0);
}

int
thrower()
{
        @try
        {
                thrower_try_body();
                @throw [Object new];
        }
        @finally
        {
                finally_body();
        }       // <----- program aborts here.
        return 0;
}


int 
main(int ac, char *av[])
{
        @try
        {
                thrower();
        }
        @catch (id exc)
        {
                printf("Got exception of class %s\n", [[exc class] name]);
                [exc free];
        }
}
