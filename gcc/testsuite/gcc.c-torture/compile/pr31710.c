typedef short SHORT;
struct v { SHORT i; };
void f(struct v *pin, struct v *pout) {
        if (pin->i == (-0x7fff)-1)
            pout->i = -pin->i;
}
