// { dg-lto-do link }
// { dg-extra-ld-options "-fwhole-program" }

template <int dim> 
struct AutoDerivativeFunction {
    virtual void gradient_list (void);
};
template <int dim>
void AutoDerivativeFunction<dim>::gradient_list (void)
{
}
template class AutoDerivativeFunction<1>;
int main() {}
