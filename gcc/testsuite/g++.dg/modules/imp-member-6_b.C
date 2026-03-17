// PR c++/124478
// { dg-additional-options "-fmodules" }
// { dg-module-cmi dedupe }

export module dedupe;
import tools;

// This requires the destructor to exist, and is placed in CMI.
myvector<DedupeFilesPath> vec;
