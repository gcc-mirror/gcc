/* { dg-do compile } */
/* { dg-options "-O3 -std=gnu++14" } */

template <typename Function>
bool run(const int item_count,
         Function && process_range,
         const int max_parallelism,
         int* progress = nullptr)
{
    if (max_parallelism <= 1)
    {
        if (progress == nullptr)
        {
            return process_range(0);
        }
        else
        {
            auto result = true;
            for (int i = 0; i != item_count && result; ++i)
            {
                (*progress)++;
                result = process_range(i);
            }
            return result;
        }
    }

    if (max_parallelism > 10)
    {
        if (progress == nullptr)
        {
            return process_range(0);
        }
        else
        {
            auto result = true;
            for (int i = 0; i != item_count && result; ++i)
            {
                (*progress)++;
                result = process_range(i);
            }
            return result;
        }
    }
    return false;
}

namespace
{
__attribute__((optimize(0))) bool worker_fun(const int)
{
    return true;
}
}

void demo(int n)
{
    for (int i = 0; i < n; ++i)
    {
        run(n, &worker_fun, n);
    }
}
