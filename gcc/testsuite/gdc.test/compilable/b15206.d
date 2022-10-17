// PERMUTE_ARGS:
// REQUIRED_ARGS: -O

void main()
{
}

struct Line
{
    double slope;
    double intercept;
}

Line nLineProjection(double[] historicData)
{
    Line projLine;
    projLine.intercept = historicData[$-1] + projLine.slope;
    return projLine;
}
