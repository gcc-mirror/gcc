/* { dg-do compile } */
/* { dg-options " -O3 -std=gnu++17 -ffinite-math-only -fdump-tree-optimized" } */

#include <algorithm>
#include <stdexcept>

#define AINLINE

class TestClass
{
public:
    AINLINE void SetValue(float value);

private:
    float m_Value;
};

AINLINE
void TestClass::SetValue(float value)
{
    if (value >= 0.0f && value <= 100.0f) {
        m_Value = value;
    }
    else {
        throw std::out_of_range("Value must be [0, 100].");
    }
}

void TestFunc(TestClass& t, float value)
{
    value = std::clamp(value, 30.0f, 50.0f);
    // When TestClass::SetValue is inlined, the exception throwing code is not eliminated.
    // Given that at this point we can prove that 'value' lies in the range [30.0f, 50.0f] well within the range required by the setter function, we can rid the not taken paths of code.
    t.SetValue(value);
}


/* { dg-final { scan-tree-dump-times "std::out_of_range::out_of_range" 1 "optimized" } } */

