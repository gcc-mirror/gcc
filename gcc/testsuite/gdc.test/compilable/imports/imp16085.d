struct Pass
{
}

struct S
{
    import imports.imp16085b : functionAndFunction, staticFunctionAndFunction,
        functionAndTemplate, templateAndTemplate; //<- private
    // public
    Pass functionAndFunction()
    {
        return Pass();
    }

    static Pass staticFunctionAndFunction()
    {
        return Pass();
    }

    Pass functionAndTemplate()
    {
        return Pass();
    }

    Pass templateAndTemplate()()
    {
        return Pass();
    }
}
