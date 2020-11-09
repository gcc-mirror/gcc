// PR c++/81660

void bar (int);

void
fn (int b)
{
  if (b)
    throw;
  try
    {
      bar (3);
    }
  catch (int)
    {
    }
  catch (int) // { dg-warning "will be caught by earlier handler" }
    {
    }
  catch (const int) // { dg-warning "will be caught by earlier handler" }
    {
    }
  catch (int &) // { dg-warning "will be caught by earlier handler" }
    {
    }
  catch (const int &) // { dg-warning "will be caught by earlier handler" }
    {
    }
}
