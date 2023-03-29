void main ()
{
    variance([1.0, 2, 3]);
}

alias meanType(T) = T;

template variance(bool stable = true)
{
    void variance(Range)(Range r, bool isPopulation = false)
    {
        .variance!(double, stable)(r, isPopulation);
    }
}

template variance(F, bool stable = true)
{
    void variance(Range)(Range r, bool isPopulation = false) {}
    void variance(scope const F[] ar...) {}
}
