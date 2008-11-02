typedef int Int32;
void use_it(int);
void FindAndReadSignature(int processedSize)
{
  int numPrevBytes = 1;
  for (;;)
    {
      int numBytesInBuffer = numPrevBytes + processedSize;
      Int32 numTests = numBytesInBuffer - 1;
      use_it (numTests);
      numPrevBytes = numBytesInBuffer - numTests;
    }
}

